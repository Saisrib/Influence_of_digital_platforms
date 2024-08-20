import pandas as pd
import matplotlib.pyplot as plt
import streamlit as st
import seaborn as sns
import numpy as np
from streamlit_option_menu import option_menu

# Load the dataset
df = pd.read_csv("C:/Users/saisr/OneDrive/Desktop/2nd sem/R/R project.csv")

# Title of the web app
st.title("Analysis of Influence of Digital Platform Usage Among Students for Academic Performances")

# Sidebar options
with st.sidebar:
    selected_option = option_menu("Select", ["Home", "Demographic Information", "Visual Representation", "Interpretation"])

# Display based on selected option
if selected_option == "Home":
    st.subheader("Introduction")
    st.write("""\
             
        Digital tools or platforms significantly influence student performance at this point. Understanding how digital platforms influence students' performance is crucial. The study explores how students' use of digital platforms impacts their academic performance. It understands the demographics of students, their digital tool usage patterns, and the impact on skills development, career paths, learning experiences, time management, and collaboration. The goal is to discover how digital tools influence students' academic performance.
        """)

    st.subheader("Objectives")
    st.write("""\
        - Investigate the impact of digital platform utilization on academic performance.
        - Understand the demographic composition of the students.
        - Analyse patterns of digital tool usage among students.
        - Evaluate how using digital tools impacts skills, career paths, learning, time management, and working together with others.
        """)

    st.subheader("Datasets")
    st.write("""\
        The dataset, comprising 205 individuals from November 2023, contains information organized into three sections: digital platform usage, learning impact, and time management and collaboration. It captures participants' engagement, learning experiences, and practical implications.
        """)
    st.subheader("Dataset:")
    st.write(df)  # Displaying the dataset

elif selected_option == "Demographic Information":
    st.write("Demographic Information:")
    
    selected_variable = option_menu("Select a demographic variable", ["Age", "Studying", "Gender", "Internship", "Urban/Rural"])    #for multiple options
    
    if selected_variable == "Age":
        # Histogram for Age
        st.subheader("Age Distribution")
        fig, ax = plt.subplots()
        sns.histplot(df['Age'], bins=20, kde=True)        #bins=bar, kde= density
        plt.xlabel("Age")
        plt.ylabel("Frequency")
        st.pyplot(fig)
    
    elif selected_variable == "Studying":
        # Bar plot for Studying
        st.subheader("Currently Studying")
        fig, ax = plt.subplots()
        sns.countplot(data=df, x='studying')
        plt.xlabel("Studying")
        plt.ylabel("Count")
        st.pyplot(fig)
    
    elif selected_variable == "Gender":
        # Pie plot for Gender
        st.subheader("Gender Distribution")
        gender = df['Gender'].value_counts()
        fig, ax = plt.subplots()
        ax.pie(gender, labels=gender.index, autopct='%1.1f%%', colors=['skyblue', 'lightgreen'])      #autopct used for displaying percentage
        ax.axis('equal') 
        plt.title("Gender Distribution")
        st.pyplot(fig)
    
    elif selected_variable == "Internship":
        # Donut pie plot for Internship
        st.subheader("Internship Distribution")
        internship = df['internship'].value_counts()
        fig, ax = plt.subplots()
        ax.pie(internship, labels=internship.index, autopct='%1.1f%%', colors=['skyblue', 'lightgreen'])
        circle = plt.Circle((0,0),0.75,fc='white')               
        fig.gca().add_artist(circle)        #gca get current axes,add_artist  used to update from the above plot
        ax.axis('equal')
        plt.title("Internship Distribution")
        st.pyplot(fig)
    
    elif selected_variable == "Urban/Rural":
        # Scatter plot for Urban/Rural
        st.subheader("Distribution of living in Urban and Rural areas")
        fig, ax = plt.subplots()
        sns.histplot(df['rural/urban'], bins=20, kde=True)
        plt.xlabel("Urban and Rural areas")
        plt.ylabel("Frequency")
        st.pyplot(fig)
    

elif selected_option == "Visual Representation":
    st.subheader("Visual Representation:")
    feature_x = st.selectbox("Select a feature for x axis", df.columns)
    feature_y = st.selectbox("Select a feature for y axis", df.columns)
    
    # Scatter plot
    st.subheader("Scatter Plot")
    fig, ax = plt.subplots()
    sns.scatterplot(data=df, x=feature_x, y=feature_y)
    plt.xlabel(feature_x)
    plt.ylabel(feature_y)
    plt.title("Scatter Plot")
    st.pyplot(fig)
    
    # Bar plot
    st.subheader("Bar Plot")
    fig, ax = plt.subplots()
    sns.countplot(data=df, x=feature_x)
    plt.xlabel(feature_x)
    plt.ylabel("Count")
    plt.title("Bar Plot")
    st.pyplot(fig)
    
    # Pie plot
    st.subheader("Pie Plot")
    pie_counts = df[feature_y].value_counts()
    fig, ax = plt.subplots()
    ax.pie(pie_counts, labels=pie_counts.index, autopct='%1.1f%%')      
    ax.axis('equal')
    plt.title("Pie Plot")
    st.pyplot(fig)

    # Line plot
    st.subheader("Line Plot")
    fig, ax = plt.subplots()
    sns.lineplot(data=df, x=feature_x, y=feature_y)
    plt.xlabel(feature_x)
    plt.ylabel(feature_y)
    plt.title("Line Plot")
    st.pyplot(fig)

    # Box plot
    st.subheader("Box Plot")
    fig, ax = plt.subplots()
    sns.boxplot(data=df, x=feature_x, y=feature_y)
    plt.xlabel(feature_x)
    plt.ylabel(feature_y)
    plt.title("Box Plot")
    st.pyplot(fig)
    
    # Violin plot
    st.subheader("Violin Plot")
    fig, ax = plt.subplots()
    sns.violinplot(data=df, x=feature_x, y=feature_y)
    plt.xlabel(feature_x)
    plt.ylabel(feature_y)
    plt.title("Violin Plot")
    st.pyplot(fig)


elif selected_option == "Interpretation":
    st.subheader("Interpretation:")
    
    interpretation_text = """
    ## Insights from Data Analysis:
    
    1. **Improved Interactions**: The data reveals that students acknowledge an improvement in their interactions with classmates due to learning from digital platforms. This indicates that digital learning not only enhances academic performance but also fosters better collaboration among peers.
    
    2. **Personalized Learning**: A significant finding is that students perceive a personalized learning experience offered by digital platforms, which positively influences their academic performance. This suggests that tailored learning experiences catered to individual needs contribute to better learning outcomes.
    
    3. **Impact on Part-Time Working Students**: The analysis highlights that digital tools and websites play a crucial role in helping part-time working students manage academic tasks efficiently, leading to improved grades despite their work commitments.
    
    4. **Preference for Digital Learning**: While the majority of students find digital platform learning beneficial, some still express a preference for traditional methods. This indicates a diverse perspective among students regarding the effectiveness of digital learning compared to conventional approaches.
    
    5. **Active Participation in Online Discussions**: The data underscores the significance of active participation in online discussions on educational platforms, contributing to better learning and grades. This suggests that engagement in collaborative learning environments enhances academic performance.
    
    6. **Positive View of Digital Tools**: Overall, students hold a generally positive view of using digital tools for various purposes, including learning, time management, collaboration, and accessing resources. This reflects the perceived utility and effectiveness of digital platforms in supporting academic endeavors.
    
    7. **Usage Patterns among College Students and Urban Dwellers**: The analysis reveals that college students and urban dwellers tend to use digital platforms more extensively for academic purposes. This underscores the importance of digital access and familiarity in certain demographic groups.
    
    8. **Balancing Productivity and Distractions**: While digital platforms aid in productivity and time management, they can also lead to distractions, particularly among college students. This highlights the need for strategies to mitigate distractions while leveraging digital tools for academic purposes.
    
    9. **Correlations in Platform Usage**: Strong correlations exist between using digital platforms for collaboration, assignments, productivity perceptions, and time management. This indicates an interconnected relationship between various aspects of platform usage and academic performance.
    
    10. **Demographic Correlations with Academic Performance**: The data suggests that digital platform usage correlates with improved academic performance across various demographics. This implies that leveraging digital tools can have a positive impact on academic outcomes across diverse student groups.
    
    ## Implications and Recommendations:
    
    1. **Enhancing Collaboration and Interaction**: Institutions can leverage digital platforms to facilitate collaborative learning environments and enhance interactions among students, thereby fostering a conducive learning environment.
    
    2. **Tailoring Learning Experiences**: Educators should focus on providing personalized learning experiences through digital platforms to cater to individual student needs and preferences, ultimately improving academic performance.
    
    3. **Supporting Part-Time Working Students**: Academic institutions should provide support services and resources tailored to the needs of part-time working students, helping them effectively manage academic tasks alongside their work commitments.
    
    4. **Promoting Digital Literacy**: There is a need to promote digital literacy skills among students to ensure they can effectively utilize digital tools for academic purposes and future career opportunities.
    
    5. **Balancing Digital and Traditional Methods**: Institutions should adopt a balanced approach that integrates digital learning with traditional methods to accommodate diverse learning preferences and optimize learning outcomes.
    
    6. **Mitigating Distractions**: Strategies should be implemented to mitigate distractions associated with digital platforms, such as providing guidelines for effective time management and minimizing non-academic content.
    
    7. **Continuous Assessment and Feedback**: Institutions should leverage digital platforms for real-time assessment and feedback, allowing for continuous monitoring of student progress and facilitating improvements in learning outcomes.
    
    8. **Enhancing Multimedia Integration**: Integrating multimedia content in digital learning platforms can enhance engagement and comprehension among students, enriching the learning experience and promoting deeper understanding of subject matter.
    
    These insights and recommendations provide valuable guidance for educational institutions, educators, and students to maximize the benefits of digital platforms in supporting academic endeavors and improving learning outcomes.
    """
    
    st.markdown(interpretation_text)



